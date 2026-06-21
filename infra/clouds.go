package infra

import "fmt"

// The cloud emitters lower the agnostic resource graph to OpenTofu/Terraform HCL.
// The emitted HCL is consumed by EITHER `tofu` or `terraform` (the language is
// shared; only the CLI binary and provider registry differ), so one emitter per
// provider serves both IaC tools. Each emitter realizes EXACTLY the agnostic
// LogicalResource set — the cloud-specific scaffolding (resource groups, Service Bus
// namespaces, Pub/Sub subscriptions) is provider plumbing, not part of the logical
// set the equivalence gate compares.

// unsupported reports a resource kind a target does not yet lower (KV/Object land
// after Queue in slice 1). A clear error, never a panic.
func unsupported(target string, r Resource) error {
	return fmt.Errorf("infra: target %q does not yet support the %q resource %q",
		target, r.Kind(), r.LogicalName())
}

// AWS lowers to OpenTofu/Terraform HCL for Amazon Web Services.
type AWS struct{}

func (AWS) Target() string { return "aws" }
func (AWS) Cloud() bool    { return true }

func (a AWS) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("aws =")
	h.attr("source", str("hashicorp/aws"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"aws_region\"")
	h.attr("type", "string")
	h.attr("default", str("us-east-1"))
	h.close()
	h.blank()
	h.open("provider \"aws\"")
	h.attr("region", "var.aws_region")
	h.close()
	for _, r := range rs {
		h.blank()
		q, ok := r.(Queue)
		if !ok {
			return Artifact{}, unsupported("aws", r)
		}
		h.open("resource \"aws_sqs_queue\" %s", str(q.Name))
		h.attr("name", str(q.Name))
		if q.FIFO {
			h.attr("fifo_queue", "true")
		}
		h.close()
	}
	return Artifact{Files: map[string]string{"main.tf": h.String()}, Logical: logicalSet(rs)}, nil
}

// Azure lowers to OpenTofu/Terraform HCL for Microsoft Azure. A Service Bus queue
// needs a resource group + a namespace (shared plumbing), then one queue each.
type Azure struct{}

func (Azure) Target() string { return "azure" }
func (Azure) Cloud() bool    { return true }

func (az Azure) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("azurerm =")
	h.attr("source", str("hashicorp/azurerm"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"azure_location\"")
	h.attr("type", "string")
	h.attr("default", str("eastus"))
	h.close()
	h.blank()
	h.open("provider \"azurerm\"")
	h.emptyBlock("features")
	h.close()
	h.blank()
	h.open("resource \"azurerm_resource_group\" \"wavelet\"")
	h.attr("name", str("wavelet-rg"))
	h.attr("location", "var.azure_location")
	h.close()
	h.blank()
	h.open("resource \"azurerm_servicebus_namespace\" \"wavelet\"")
	h.attr("name", str("wavelet-ns"))
	h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
	h.attr("location", "azurerm_resource_group.wavelet.location")
	h.attr("sku", str("Standard"))
	h.close()
	for _, r := range rs {
		h.blank()
		q, ok := r.(Queue)
		if !ok {
			return Artifact{}, unsupported("azure", r)
		}
		h.open("resource \"azurerm_servicebus_queue\" %s", str(q.Name))
		h.attr("name", str(q.Name))
		h.attr("namespace_id", "azurerm_servicebus_namespace.wavelet.id")
		if q.FIFO {
			h.attr("requires_session", "true")
		}
		h.close()
	}
	return Artifact{Files: map[string]string{"main.tf": h.String()}, Logical: logicalSet(rs)}, nil
}

// GCP lowers to OpenTofu/Terraform HCL for Google Cloud. A Pub/Sub queue is a topic
// + a pull subscription (the subscription is plumbing, not a separate logical
// resource).
type GCP struct{}

func (GCP) Target() string { return "gcp" }
func (GCP) Cloud() bool    { return true }

func (g GCP) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("google =")
	h.attr("source", str("hashicorp/google"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"gcp_project\"")
	h.attr("type", "string")
	h.close()
	h.blank()
	h.open("variable \"gcp_region\"")
	h.attr("type", "string")
	h.attr("default", str("us-central1"))
	h.close()
	h.blank()
	h.open("provider \"google\"")
	h.attr("project", "var.gcp_project")
	h.attr("region", "var.gcp_region")
	h.close()
	for _, r := range rs {
		h.blank()
		q, ok := r.(Queue)
		if !ok {
			return Artifact{}, unsupported("gcp", r)
		}
		h.open("resource \"google_pubsub_topic\" %s", str(q.Name))
		h.attr("name", str(q.Name))
		h.close()
		h.blank()
		h.open("resource \"google_pubsub_subscription\" %s", str(q.Name+"_sub"))
		h.attr("name", str(q.Name+"-sub"))
		h.attr("topic", "google_pubsub_topic."+q.Name+".name")
		h.close()
	}
	return Artifact{Files: map[string]string{"main.tf": h.String()}, Logical: logicalSet(rs)}, nil
}
